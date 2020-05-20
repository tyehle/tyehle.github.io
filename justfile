#!/usr/bin/env just --justfile

serve host="localhost":
    bundle exec jekyll serve --drafts --livereload --incremental --host={{host}}

update:
    bundle update

install:
    bundle install

build: install
    JEKYLL_ENV=production bundle exec jekyll build

clean:
    rm -rf _site/ .jekyll-cache/ .sass-cache/

@post title:
    #!/usr/bin/env bash
    filename="_drafts/{{title}}.md"
    printf -- "---\nlayout: post\ntitle: {{title}}\n---\n" > $filename
    code $filename

transcode-video input output:
    ffmpeg -i {{input}} -an -c:v h264 -pix_fmt yuv420p -crf 5 -tune animation -preset veryslow -movflags +faststart {{output}}
