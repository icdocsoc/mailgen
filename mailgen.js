#!/usr/bin/env node

"use strict";

// Dependencies
const fs = require('fs'),
      xsd = require('libxml-xsd'),
      libxmljs = xsd.libxmljs

// Load and parse the schema
const emailSchemaContents = readFileSyncOrExit("email.xsd", "schema")
const schema = xsd.parse(emailSchemaContents)

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

// Validate against the schema
const result = schema.validate(emailXMLContents)
if (result !== null) {
  console.error("Email XML is invalid.")
  console.error(result)
  process.exit(3)
}

// Parse the XML
const emailXMLDoc = libxmljs.parseXmlString(emailXMLContents)

console.dir(emailXMLDoc)
