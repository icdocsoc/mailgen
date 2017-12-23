"use strict";

const fb         = require('./fb.js'),
      sponsors   = require('./sponsors.js')

module.exports.EmailMeta = class EmailMeta {
  constructor(logos = true, agenda = true, social = true) {
    this.logos = logos
    this.agenda = agenda
    this.social = social
  }
}

module.exports.EmailHeading = class EmailHeading {
  constructor(text) {
    this.text = text
  }

  get template() {
    return "heading.html"
  }

  templateData(md) {
    return {
      "text": md.render(`# ${this.text}`)
    }
  }
}

module.exports.EmailText = class EmailText {
  constructor(text) {
    this.text = text
  }

  get template() {
    return "text.html"
  }

  templateData(md) {
    return {
      "text": this.text
    }
  }
}

module.exports.DateRange = class DateRange {
  // Start and end should be moment objects
  constructor(start, end) {
    this.start = start
    this.end = end
  }
}

module.exports.EmailEvent = class EmailEvent {
  constructor(date = null, location, external = false) {
    this.date = date
    this.location = location
    this.external = external

    this.image = null
    this.facebook = null
    this.links = null
    this.text = null
  }

  get hasDate() {
    return date !== null
  }

  get hasImage() {
    return this.image !== null
  }

  get hasFacebook() {
    return this.facebook !== null
  }

  fetch() {
    // TODO: perform the fetch and the update
  }

  get hasLinks() {
    return this.links !== null
  }

  get hasText() {
    return this.text !== null
  }

  get template() {
    return "event.html"
  }

  templateData(md) {
    const formattedDates = (() => {
      // Check whether we have a single-day event
      if (this.date.start.year() === this.date.end.year() &&
        this.date.start.month() === this.date.end.month() &&
        this.date.start.date() === this.date.end.date()) {

        return this.date.start.format("Do MMMM YYYY, HH.mm-") +
          this.date.end.format("HH.mm")
      }

      return this.date.start.format("Do MMMM YYYY, HH.mm-") +
        this.date.end.format("Do MMMM YYYY, HH.mm")
    })()

    return {
      "header": this.image,
      "header_alt": "TODO: HEADER ALT",
      "day": this.date.start.date(),
      "month": this.date.start.format("MMMM"),
      "date": formattedDates,
      "location": this.location,
      "plural_links": this.hasLinks ?  this.links.length > 1 : false,
      "links": this.links,
      "description": md.render(this.text),
      "external": this.external
    }
  }
}

module.exports.EmailEventLink = class EmailEventLink {
  constructor(href, text) {
    this.href = href
    this.text = text
  }
}

module.exports.EmailEventFacebook = class EmailEventFacebook {
  constructor(id, alias = null) {
    this.id = id
    this.alias = alias
  }

  get hasAlias() {
    return this.alias !== null
  }
}

module.exports.EmailSponsor = class EmailSponsor {
  constructor(company, text) {
    this.company = company
    this.text = text
  }

  get template() {
    return "sponsor.html"
  }

  templateData(md) {
    return {
      "company": this.company,
      "company_name": sponsors[this.company],
      "content": md.render(this.text)
    }
  }
}

module.exports.EmailImage = class EmailImage {
  constructor(src, width = 100, alt = null, title = null) {
    this.src = src
    this.width = width
    this.alt = alt
    this.title = title
  }

  get hasAlt() {
    return this.alt !== null
  }

  get hasTitle() {
    return this.title !== null
  }

  get template() {
    return "image.html"
  }

  templateData(md) {
    return {
      "width": this.width,
      "src": this.src,
      "alt": this.alt,
      "title": this.title
    }
  }
}
