#!/usr/bin/env just --justfile

serve:
    bundle exec jekyll serve --drafts

update:
    bundle update

install:
    bundle install

@post title:
    #!/usr/bin/env bash
    filename="_drafts/{{title}}.md"
    printf -- "---\nlayout: post\ntitle: {{title}}\n---\n" > $filename
    code $filename
