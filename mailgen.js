#!/usr/bin/env node

"use strict";

// Dependencies
const fs = require('fs'),
      libxml = require('libxmljs')

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
const emailXMLContents = readFileSyncOrExit(process.argv[2], "email")

// Parse the XML
const emailXMLDoc = libxml.parseXmlString(emailXMLContents)

// Validate against the schema
if (!emailXMLDoc.validate(schema)) {
  console.error("Email XML is invalid.")
  console.error(emailXMLDoc.validationErrors)
  process.exit(3)
}

console.dir(emailXMLDoc)
