# couch_image_resizer #

`couch_image_resizer` is a simple web service that can serve CouchDB document attachments and, if the attachment is an image, is able to resize it using the `convert` program that is part of the `ImageMagick` tool suite. Resized images are held in an in-memory LRU cache, whose configurable size is 128 megabytes by default.

## Requirements ##

* Erlang (it has successfully been tested with versions >= R14B)
* CouchDB (it is intended to be used via CouchDB's HTTP proxy API, which is available in versions >= 1.1.0)
* ImageMagick

## Installation ##

In order to run `couch_image_resizer` with its default settings, you just have to compile it, start it, and configure CouchDB accordingly.

**Example:**

```Shell
cd couch_image_resizer
make
./start.sh
curl -X PUT \
     -d '"{couch_httpd_proxy, handle_proxy_req, <<\"http://127.0.0.1:5985\">>}"' \
     "http://admin:secret@127.0.0.1:5984/_config/httpd_global_handlers/_image"
```

Alternatively, instead of the last line above, the CouchDB proxy handler can also be configured via Futon or the CouchDB configuration file `local.ini`. Simply add a section like the following under `[httpd_global_handlers]`:

```INI
[httpd_global_handlers]
_image = {couch_httpd_proxy, handle_proxy_req, <<"http://127.0.0.1:5985">>}
```

## Configuration ##

There are two configuration files located in `config/`: `elog.config` and `couch_image_resizer.erlenv`. The former contains the configuration for the Erlang logger, while the latter contains all other configuration, including the path of the Erlang logger configuration (which is `config/elog.config` by default).

When invoked without any argument, `start.sh` uses `config/couch_image_resizer.erlenv` by default. In order to make it use a different configuration file, simply pass it the configuration file path as an argument.

## Usage ##

Here, we assume that `couch_image_resizer` is configured as `_image` proxy handler in your CouchDB, which is running locally on the default port 5984.

Assume that you have got the image `profile.jpg` attached to your document `user` in your database `db2`, and you would like to resize it to a size of 1024 * 512 pixel, with its original aspect ratio being ignored. All you have to do, in order to get the scaled version of the image, is to prepend `/_image` to the path and add a query parameter `resize`, whose value specifies the desired image geometry. That is, instead of `http://127.0.0.1:5984/db2/user/profile.jpg`, simply request `http://127.0.0.1:5984/_image/db2/user/profile.jpg?resize=1024x512!`.

Everything that is a [valid ImageMagick geometry](http://www.imagemagick.org/script/command-line-processing.php?ImageMagick=e43g49ls83iklaldahr2bb9l06#geometry) is supported for the `resize` query parameter value. For instance, if you would like the image to have an area of exactly 400 pixel, simply specify `400@` as the `resize` query parameter value.

As an alternative to the `resize` query parameter value, the geometry can also be specified as `X-Imagemagick-Resize` request header value.

## Credits ##

`couch_image_resizer` is built off the following libraries:

* [MochiWeb](https://github.com/mochi/mochiweb)
* [ibrowse](https://github.com/cmullaparthi/ibrowse)
* [term_cache](https://github.com/fdmanana/term_cache)

Kudos to Bob, Chandrashekhar, and Filipe, but also to everybody else who contributed to those great pieces of code!
