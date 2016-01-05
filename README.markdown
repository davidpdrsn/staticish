# Staticish

**NOTE:** Its currently still in development and is not ready for production use.

This is an experimental static-ish site generator.

The idea behind this project is that I would like a static site generator that also handles dynamic content in the form of JSON responses. The hope is to make something that is as easy to use as a static site generator but still supports features like searching and other dynamic things.

## Todo
- [x] Add support for some kind of layout file.
- [x] Don't show logging output from the main thread. Spawn new processes and somehow prevent them from writing all at the same time.
- [x] Add static views.
- [ ] Add dynamic routes that respond with JSON.
- [ ] Make default JSON endpoint that just returns all the posts.
- [ ] Add script for quickly adding new posts with format like "2015-01-15-title-goes-here.markdown".
- [ ] Get syntax highlighting of code snippets in posts.
- [ ] Make executable that can be run from any directory with "posts" and "views" folders.
- [ ] Add optional to executable to generate the required folders. At the moment that would be "posts" and "views".
- [ ] Track the amount of time it look for a request to complete and show that on the log.
