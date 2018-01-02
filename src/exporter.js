#!/usr/bin/env node

"use strict";

const Mustache = require('mustache'),
      fs       = require('fs'),
      uuid     = require('uuid/v4'),
      cheerio  = require('cheerio')

const md = require('markdown-it')({
  html:     true,
  xhtmlOut: true,
  breaks:   false,
  linkify:  true
})

// Turn off mustache escaping
Mustache.escape = v => v

function renderBlock(block) {
  const contents = fs.readFileSync(`resources/components/${block.template}`, {
    "encoding": "utf8"
  })
  return Mustache.render(contents, block.templateData(md))
}

module.exports.render = (meta, content) => {
  // Render each of the blocks
  const blocks = content.map(b => renderBlock(b)).join("")

  let agendaItems = []
  if (meta.agenda) {
    const $ = cheerio.load(blocks)
    agendaItems = cheerio.load(blocks)("h1:not(.title)").toArray().map((x) => {
      return $(x).text() })
  }

  const contents = fs.readFileSync("resources/template.html", {"encoding": "utf8"})
  return Mustache.render(contents, {
    "has_social": meta.social,
    "has_logos": meta.logos,
    "has_agenda": meta.agenda,
    "title": meta.title,
    "agenda": agendaItems,
    "content": blocks,
    "uuid": encodeURIComponent(uuid())
  })
}
