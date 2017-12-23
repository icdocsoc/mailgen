#!/usr/bin/env node

"use strict";

const Mustache = require('mustache'),
      fs       = require('fs')

const md = require('markdown-it')({
  html:     true,
  xhtmlOut: true,
  breaks:   true,
  linkify:  true
})

// Turn off mustache escaping
Mustache.escape = v => v

function renderBlock(block) {
  const contents = fs.readFileSync(`resources/templates/components/${block.template}`, {
    "encoding": "utf8"
  })
  return Mustache.render(contents, block.templateData(md))
}

module.exports.render = (meta, content) => {
  // Render each of the blocks
  const blocks = content.map(b => renderBlock(b)).join("")

  const contents = fs.readFileSync("resources/templates/template.html", {"encoding": "utf8"})
  return Mustache.render(contents, {
    "content": blocks
  })
}
