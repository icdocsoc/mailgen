#!/usr/bin/env node

"use strict";

// Dependencies
const fs            = require('fs'),
      libxml        = require('libxmljs'),
      moment        = require('moment'),
      path          = require('path'),
// Own dependencies
      types         = require('./email.js'),
      sponsors      = require('./sponsors.js'),
      exporter      = require('./exporter.js')

// Load and parse the schema
const emailSchemaContents = readFileSyncOrExit("email.xsd", "schema")
const schema = libxml.parseXmlString(emailSchemaContents)

// Ensure that we receive the correct command-line arguments
if (process.argv.length <= 2) {
  console.error("Please provide email document as an argument!")
  console.log("Usage ./mailgen.js <path to email.xml>")
  process.exit(1)
}

function readFileSyncOrExit(path, kind) {
  try {
    return fs.readFileSync(path, {encoding: "utf8"})
  } catch (err) {
    console.error(`Cannot read ${kind} file\n`, err)
    process.exit(2)
  }
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

function attrOrDefaultValue(node, attribute, def) {
  const attr = node.attr(attribute)
  if (attr == null) {
    return def
  }

  return attr.value()
}

// Extension specific to XML reader
types.EmailMeta.constructWithNode = node => {
  return new types.EmailMeta(
    attrOrDefaultValue(node, "logos", "true") === "true",
    attrOrDefaultValue(node, "agenda", "true") === "true",
    attrOrDefaultValue(node, "social", "true") === "true"
  )
}

types.EmailHeading.constructWithNode = node => {
  return new types.EmailHeading(node.text())
}

types.EmailText.constructWithNode = node => {
  return new types.EmailText(node.text())
}

types.EmailEvent.constructWithNode = node => {
  const emailEvent = new types.EmailEvent((() => {
      const startDate = attrOrDefaultValue(node, "startDate", null)
      const endDate = attrOrDefaultValue(node, "endDate", null)
      return startDate !== null && endDate !== null ?
        new types.DateRange(moment(startDate), moment(endDate)) : null
    })(),
    attrOrDefaultValue(node, "location", ""),
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
        emailEvent.text = child.text()
        break
    }
  }

  return emailEvent
}

types.EmailEventLink.constructWithNode = node => {
  return new types.EmailEventLink(node.attr("href").value(),
    node.text())
}

types.EmailEventFacebook.constructWithNode = node => {
  return new types.EmailEventFacebook(node.attr("id").value(),
    attrOrDefaultValue(node, "alias", null))
}

types.EmailSponsor.constructWithNode = node => {
  return new types.EmailSponsor(node.attr("company").value(),
    node.text())
}

types.EmailImage.constructWithNode = node => {
  return new types.EmailImage(node.attr("src").value(),
    parseInt(attrOrDefaultValue(node, "width", "100")),
    attrOrDefaultValue(node, "alt", null),
    attrOrDefaultValue(node, "title", null))
}

// Get email metadata
const emailMeta = types.EmailMeta.constructWithNode(emailXMLDoc.root())
const emailTypeFields = {
  "heading" : types.EmailHeading,
  "text"    : types.EmailText,
  "event"   : types.EmailEvent,
  "sponsor" : types.EmailSponsor,
  "image"   : types.EmailImage
}

const emailData = emailXMLDoc.childNodes().map(x =>
  emailTypeFields[x.name()].constructWithNode(x))

// Update the facebook events with facebook information
/*{
  const facebookEvents = emailData.filter(x =>
    x instanceof types.EmailEvent && x.hasFacebook)
  for (const fbe of facebookEvents) {
    fbe.fetch()
  }
}*/

// Generate the markdown

const markdown = exporter.render(emailMeta, emailData)
const newEmailFileName = path.join(path.dirname(emailFileName),
  path.basename(emailFileName, ".xml") + ".html")

fs.writeFileSync(newEmailFileName, markdown, {"encoding": "utf8"})

console.log(`Wrote email to: ${newEmailFileName}`)
