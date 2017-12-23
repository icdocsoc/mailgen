#!/usr/bin/env node

"use strict";

const graph    = require('fbgraph'),
      readline = require('readline'),
      jsonfile = require('jsonfile')

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

console.log("Let's get authenticated!")

const conf = jsonfile.readFileSync("config.json")

const authenticationURL = graph.getOauthUrl({
  "client_id": conf.client_id,
  "redirect_uri": conf.redirect_uri,
})

console.log("Visit this URL in your web browser:")
console.log(authenticationURL)

rl.question("Enter the code received:\n", code => {
  graph.authorize({
    "client_id": conf.client_id,
    "redirect_uri": conf.redirect_uri,
    "client_secret": conf.client_secret,
    "code": code
  }, function (err, facebookRes) {
    rl.close()
    if (err !== null) {
      console.log("Encountered an error")
      console.log(err)

      return
    }

    conf.access_token = facebookRes.access_token
    console.log("Got access token! Writing config...")
    jsonfile.writeFileSync("config.json", conf)
  });
})
