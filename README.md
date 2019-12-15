# notionfy: 📓 to 🗒 -> ♥

### Sync your Kindle highlights to [Notion](https://www.notion.so/)

This script reads the `clippings.txt` from your kindle and syncs all the highlight to a selected notion page.

### Install

### Linux

Download the [notionfy](https://github.com/yannick-cw/notionfy/releases/tag/0.1) executable (for linux or default for Mac) and place it in your `PATH` or run it directly with `./notionfy`. Don't forget to make it executable with `chmod +x notionfy`.

On Mac you may also give it permission to run in `System Preferences -> Security & Privacy`

#### Mac
On Mac you can use homebrew
```
brew install yannick-cw/homebrew-tap/notionfy
```

#### Windows

1. Download the zip for the latest windows [release](https://github.com/yannick-cw/notionfy/releases/download/0.1/notionfy_win.zip) file to .e.g `Downloads`
2. Right click extract all (to any folder you choose)
3. Open power shell (or any shell)
4. Change Directory to the unzipped path, e.g.: `cd .\Downloads\notionfy_win\`
5. `.\notionfy.exe`
Should give you the outcome
```
Usage: .....
```
That means it works so far.
Now run it with your configuration:

```
.\notionfy.exe --token "TOKEN_HERE" --page "PAGE_ID_HERE" --kindle "D:"
```

Where token is token form the cookie and page id from the url of the page you want to add the snippets. When I connect my kindle to a windows machine it is mounted as `D:` so check under what path you kindle is mounted and add that instead of `:D`

### Setup

1. Get the `token_v2` token from https://www.notion.so/

- when using chrome [here](https://developers.google.com/web/tools/chrome-devtools/storage/cookies) is some info on how to read a cookie

2. Create a new, empty page and copy the id

- e.g. `https://www.notion.so/Kindle-Highlights-5129b8f88a414b8e893469b2d95daac8`
- take `5129b8f88a414b8e893469b2d95daac8`

3. Connect you kindle to your machine and get the path to the kindle (on Mac this is `/Volumes/Kindle`)
4. run `notionfy` with:

```bash
notionfy -n "notion_token" -p "parent_page_id" -k "kindle_path"
```

5. See the highlights added to notion page

![Highlights](./highlights.png)

