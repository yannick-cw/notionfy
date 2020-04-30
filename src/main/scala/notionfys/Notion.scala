package notionfys

import App._
import sttp.client._
import sttp.client.circe._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import cats.effect.IO
import java.{util => ju}
import cats.MonadError

trait Notion[F[_]] {
  def addHighlight(userId: ju.UUID): Highlight => F[Unit]
  def getHighlights: F[(List[Highlight], ju.UUID)]
}
object Notion extends Notion[AppM] {
  implicit val backend  = HttpURLConnectionBackend()
  private val notionUrl = "https://www.notion.so/api/v3"
  private val noIdErr: AppM[ju.UUID] = MonadError[AppM, Throwable].raiseError(
    new RuntimeException("Did not find a notion userId in pageChunkResponse")
  )
  private def randomId = ju.UUID.randomUUID

  def addHighlight(userId: ju.UUID): Highlight => AppM[Unit] =
    highlight =>
      for {
        Args(token, page, _, _) <- AppM.ask[IO, Args]
        (headerId, contentId, seperatorId) = (randomId, randomId, randomId)
        titlePart = List(
          addSegment(headerId, page, userId, "sub_header"),
          addAfter(headerId, page),
          addContent(headerId, highlight.title)
        )
        contentPart = List(
          addSegment(contentId, page, userId, "text"),
          addAfter(contentId, page),
          addContent(contentId, highlight.content)
        )
        seperator = List(
          addSegment(seperatorId, page, userId, "divider"),
          addAfter(seperatorId, page)
        )
        reqBody = Transaction(titlePart ++ contentPart ++ seperator)
        res     = reqRes[Transaction, Unit](reqBody, "submitTransaction", token)
        _ <- Console.verboseLog(s"Responded with code: ${res.code}")
      } yield ()

  private def addAfter(opId: ju.UUID, parentId: ju.UUID) =
    Operation(
      id = parentId,
      path = List("content"),
      command = "listAfter",
      table = "block",
      args = ObjArgs(opId)
    )
  private def addContent(opId: ju.UUID, content: String) =
    Operation(
      id = opId,
      path = List("properties", "title"),
      command = "set",
      table = "block",
      args = ArrayArgs(List(List(content)))
    )
  private def addSegment(opId: ju.UUID, parentId: ju.UUID, userId: ju.UUID, `type`: String) =
    Operation(
      id = opId,
      path = List.empty,
      command = "set",
      table = "block",
      args = ObjArgs(id = opId, createdBy = userId, parentId = parentId, `type` = `type`)
    )

  def getHighlights: AppM[(List[Highlight], ju.UUID)] =
    for {
      Args(token, page, _, _) <- AppM.ask[IO, Args]
      reqBody = PageChunkRequest(page)
      res     = reqRes[PageChunkRequest, PageChunkResponse](reqBody, "loadPageChunk", token)
      pageChunkResponse <- AppM.liftF(IO.fromEither(res.body))
      highligts = extractHighlights(pageChunkResponse, page)
      userId <- pageChunkResponse.recordMap.notion_user.keys.headOption.fold(noIdErr)(AppM.pure(_))
    } yield (highligts, userId)

  private def reqRes[A: Encoder, Res: Decoder](body: A, path: String, token: String) =
    basicRequest
      .post(uri"$notionUrl/$path")
      .cookie("token_v2", token)
      .body(body)
      .response(asJson[Res])
      .send()

  private def extractHighlights(response: PageChunkResponse, pageId: ju.UUID): List[Highlight] =
    for {
      contents <- response.recordMap.block.get(pageId).toList.flatMap(_.value.content)
      (title, content) <- contents
        .flatMap(cId => response.recordMap.block.get(cId).toList.map(_.value))
        .grouped(3)
        .toList
        .collect {
          case (List(
              Value(Some("sub_header"), Some(propsT), _),
              Value(Some("text"), Some(propsH), _),
              Value(Some("divider"), None, _)
              )) =>
            (propsT, propsH)
        }
      titleA <- title.title.toList.flatMap(_.headOption.toList.flatMap(_.headOption.toList))
      contentA <- content.title.toList
        .flatMap(_.headOption.toList.flatMap(_.headOption.toList))
    } yield Highlight(titleA, contentA, List.empty)
}

sealed trait Arg
object Arg {
  implicit val encoder: Encoder[Arg] = Encoder.instance {
    case arr: ArrayArgs => arr.asJson
    case obj: ObjArgs   => obj.asJson
  }
}

case class ArrayArgs(path: List[List[String]]) extends Arg
object ArrayArgs {
  implicit val encoder: Encoder[ArrayArgs] = Encoder[List[List[String]]].contramap(_.path)
}

case class ObjArgs(
    id: ju.UUID,
    version: Option[Int],
    alive: Option[String],
    created_by: Option[ju.UUID],
    parent_id: Option[ju.UUID],
    parent_table: Option[String],
    `type`: Option[String]
) extends Arg
object ObjArgs {
  implicit val encoder: Encoder[ObjArgs] = deriveEncoder[ObjArgs].mapJson(_.dropNullValues)
  def apply(id: ju.UUID): ObjArgs        = ObjArgs(id, None, None, None, None, None, None)
  def apply(id: ju.UUID, createdBy: ju.UUID, parentId: ju.UUID, `type`: String): ObjArgs =
    ObjArgs(
      id,
      Some(1),
      Some("True"),
      Some(createdBy),
      Some(parentId),
      Some("block"),
      Some(`type`)
    )
}

case class Operation(id: ju.UUID, path: List[String], command: String, table: String, args: Arg)
object Operation { implicit val encoder: Encoder[Operation] = deriveEncoder }

case class Transaction(operations: List[Operation])
object Transaction { implicit val encoder: Encoder[Transaction] = deriveEncoder }

// Response
case class Properties(title: Option[List[List[String]]])
object Properties { implicit val decoder: Decoder[Properties] = deriveDecoder }
case class Value(
    `type`: Option[String],
    properties: Option[Properties],
    content: Option[List[ju.UUID]]
)
object Value { implicit val decoder: Decoder[Value] = deriveDecoder }
case class Block(value: Value)
object Block { implicit val decoder: Decoder[Block] = deriveDecoder }
case class RecordMap(block: Map[ju.UUID, Block], notion_user: Map[ju.UUID, Block])
object RecordMap { implicit val decoder: Decoder[RecordMap] = deriveDecoder }
case class PageChunkResponse(recordMap: RecordMap)
object PageChunkResponse { implicit val decoder: Decoder[PageChunkResponse] = deriveDecoder }

// Request
case class Stack(table: String, id: ju.UUID, index: Int)
object Stack { implicit val encoder: Encoder[Stack] = deriveEncoder }

case class Cursor(stack: List[List[Stack]])
object Cursor { implicit val encoder: Encoder[Cursor] = deriveEncoder }

case class PageChunkRequest(
    pageId: ju.UUID,
    limit: Int,
    cursor: Cursor,
    chunkNumber: Int,
    verticalColumns: Boolean
)
object PageChunkRequest {
  implicit val encoder: Encoder[PageChunkRequest] = deriveEncoder
  def apply(pageId: ju.UUID): PageChunkRequest =
    PageChunkRequest(
      pageId = pageId,
      limit = 100000,
      cursor = Cursor(stack = List(List(Stack(table = "block", id = pageId, index = 0)))),
      chunkNumber = 0,
      verticalColumns = false
    )
}
