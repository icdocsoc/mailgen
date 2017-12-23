"use strict";

const graph    = require('fbgraph'),
      jsonfile = require('jsonfile')

// Set the access token
graph.setAccessToken(jsonfile.readFileSync("config.json").access_token);

module.exports = id => {
  graph.get("137844650270705?fields=cover,description,start_time,end_time,event_times,name,place,timezone", function(err, res) {
    console.log(res); // { id: '4', name: 'Mark Zuckerberg'... }
  });

  // Start & end times
  // Location, parsed down into something readable.
  // Header image if available
  // Description
  return {
  }
}
