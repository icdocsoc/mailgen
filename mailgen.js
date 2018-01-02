#!/usr/bin/env node

"use strict";

// Dependencies
const fs            = require('fs'),
      libxml        = require('libxmljs'),
      moment        = require('moment'),
      path          = require('path'),
      inlineCss     = require('inline-css'),
      hminify       = require('html-minifier'),
      stripIndent   = require('strip-indent'),
// Own dependencies
      types         = require('./src/email.js'),
      sponsors      = require('./src/sponsors.js'),
      exporter      = require('./src/exporter.js')

// Extension specific to XML reader
types.EmailMeta.constructWithNode = node => {
  return new types.EmailMeta(
    attrOrDefaultValue(node, "logos", "true") === "true",
    attrOrDefaultValue(node, "agenda", "true") === "true",
    attrOrDefaultValue(node, "social", "true") === "true",
    attrOrDefaultValue(node, "title", null)
  )
}

types.EmailHeading.constructWithNode = node => {
  return new types.EmailHeading(stripIndent(node.text()))
}

types.EmailText.constructWithNode = node => {
  return new types.EmailText(stripIndent(node.text()))
}

types.EmailEvent.constructWithNode = node => {
  const emailEvent = new types.EmailEvent((() => {
      const startDate = attrOrDefaultValue(node, "startDate", null)
      const endDate = attrOrDefaultValue(node, "endDate", null)
      return startDate !== null && endDate !== null ?
        new types.DateRange(moment(startDate), moment(endDate)) : null
    })(),
    attrOrDefaultValue(node, "location", null),
    attrOrDefaultValue(node, "external", "false") === "true")

  const children = node.childNodes()
  for (const child of children) {
    switch (child.name()) {
      case "image":
        emailEvent.image = child.attr("src").value()
        break
      case "facebook":
        emailEvent.facebook = types.EmailEventFacebook.constructWithNode(child)
        break
      case "links":
        emailEvent.links = child.childNodes().map(
          c => types.EmailEventLink.constructWithNode(c))
        break
      case "text":
        emailEvent.text = stripIndent(child.text())
        break
    }
  }

  return emailEvent
}

types.EmailEventLink.constructWithNode = node => {
  return new types.EmailEventLink(node.attr("href").value(),
    stripIndent(node.text()))
}

types.EmailEventFacebook.constructWithNode = node => {
  return new types.EmailEventFacebook(node.attr("id").value(),
    attrOrDefaultValue(node, "alias", null))
}

types.EmailSponsor.constructWithNode = node => {
  return new types.EmailSponsor(node.attr("company").value(),
    stripIndent(node.text()))
}

types.EmailImage.constructWithNode = node => {
  return new types.EmailImage(node.attr("src").value(),
    parseInt(attrOrDefaultValue(node, "width", "100")),
    attrOrDefaultValue(node, "alt", null),
    attrOrDefaultValue(node, "title", null))
}

types.EmailSign.constructWithNode = node => {
  return new types.EmailSign(node.attr("name").value(),
    node.attr("role").value(),
    attrOrDefaultValue(node, "facebook", null),
    stripIndent(node.text()))
}

// Utility functions
function readFileSyncOrExit(path, kind) {
  try {
    return fs.readFileSync(path, {encoding: "utf8"})
  } catch (err) {
    console.error(`Cannot read ${kind} file\n`, err)
    process.exit(2)
  }
}

function attrOrDefaultValue(node, attribute, def) {
  const attr = node.attr(attribute)
  if (attr == null) {
    return def
  }

  return attr.value()
}

//* * * * * * MAIN * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(async () => {
  // Load and parse the schema
  const emailSchemaContents = readFileSyncOrExit("resources/email.xsd", "schema")
  const schema = libxml.parseXmlString(emailSchemaContents)

  // Ensure that we receive the correct command-line arguments
  if (process.argv.length <= 2) {
    console.error("Please provide email document as an argument!")
    console.log("Usage ./mailgen.js <path to email.xml>")
    process.exit(1)
  }

  // Read the email file
  const emailFileName = process.argv[2]
  const emailXMLContents = readFileSyncOrExit(emailFileName, "email")

  // Parse the XML
  const emailXMLDoc = libxml.parseXmlString(emailXMLContents, {
    noent: true,
    dtdload: false,
    doctype: false,
    noblanks: true
  })

  // Validate against the schema
  if (!emailXMLDoc.validate(schema)) {
    console.error("Email XML is invalid.")
    console.error(emailXMLDoc.validationErrors)
    process.exit(3)
  }


  // Get email metadata
  const emailMeta = types.EmailMeta.constructWithNode(emailXMLDoc.root())
  const emailTypeFields = {
    "heading" : types.EmailHeading,
    "text"    : types.EmailText,
    "event"   : types.EmailEvent,
    "sponsor" : types.EmailSponsor,
    "image"   : types.EmailImage,
    "sign"    : types.EmailSign
  }

  const emailData = emailXMLDoc.childNodes().map(x =>
    emailTypeFields[x.name()].constructWithNode(x))

  // Update the facebook events with facebook information
  {
    const facebookEvents = emailData.filter(x =>
      x instanceof types.EmailEvent && x.hasFacebook)
    for (const fbe of facebookEvents) {
      await fbe.fetch()
    }
  }

  // Generate the HTML

  const outHtml = await (async () => {
    // Render based on templates and extracted data
    const html = exporter.render(emailMeta, emailData)

    // Inline the CSS
    const inlined = await inlineCss(html, {
      url: `file://${__dirname}/resources/`,
      preserveMediaQueries: true,
      applyWidthAttributes: true,
      applyTableAttributes: true,
      removeHtmlSelectors: true
    })

    // Minify the HTML
    const minified = hminify.minify(inlined, {
      collapseWhitespace: true,
      minifyCSS: true,
      removeComments: true,
      removeEmptyAttributes: true,
      removeEmptyElements: true,
      removeRedundantAttributes: true,
      sortAttributes: true
    })

    return minified
  })();

  const newEmailFileName = path.join(path.dirname(emailFileName),
    path.basename(emailFileName, ".xml") + ".html")
  fs.writeFileSync(newEmailFileName, outHtml, {"encoding": "utf8"})

  console.log(`Wrote email to: ${path.resolve(newEmailFileName)}`)
})()
