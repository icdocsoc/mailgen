"use strict";

const graph    = require('fbgraph'),
      jsonfile = require('jsonfile')

// Set the access token
graph.setAccessToken(jsonfile.readFileSync("config.json").access_token)

async function facebookFetch(url) {
  return new Promise((resolve, reject) => {
    graph.get(url, (err, res) => {
      if (err !== null) {
        reject(JSON.stringify(err))
        return
      }

      resolve(res)
    })
  })
}

module.exports = async id => {
  const url =
    `${id}?fields=cover,description,start_time,end_time,event_times,name,place,timezone`
  return await facebookFetch(url)
}
