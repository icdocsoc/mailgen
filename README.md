# DoCSoc Email Generator

A HTML email generator for DoCSoc's emails. Primarily for the digest, but works
for all formats.

Totally and utterly incompatible with the [old
one](https://github.com/icdocsoc/digest-emails).

The original idea was to write it in Haskell, but Haskell lacks many of the
libraries that make it convenient to work with HTML, so now this is a NodeJS
project. I've tried to modularise it a bit but there's still more to improve.

Emails are now composed in a custom XML format, which expresses the structure of
the email. This way, many of the common email tasks can be shortcut, like
copying information from the Facebook events for the digest, as the new
generator is able to fetch this information.

A future aim is to make it fairly simple to add new components, by making them
self-contained. The manifestation of a component in the XML documents is its
tag.

## Installation & Setup

- Install nodejs and npm.
- Clone the project.
- `cd` into its directory.
- Run `npm install`.
- Set up Facebook. The generator doesn't work without this.
  - Go to <https://developers.facebook.com/apps/>.
  - Create a new app.
  - Add the 'Facebook Login' product.
    - Provide a dummy website URL.
    - Go to the settings for 'Facebook Login'.
    - Add a dummy OAuth redirect URI.
      - Copy this to the `redirect_uri` field of `config.json`.
    - Save.
  - Go to the app settings.
    - Copy the App ID to the `client_id` field of `config.json`.
    - Copy the App Secret to the `client_secret` field of `config.json`.
  - Run `./fbsetup.js` and follow the instructions. NB: The code is returned in
    the redirect URL as a GET parameter.
  - Make sure not to share the contents of `config.json`.
- Done. Run `./mailgen.js <path to xml>` to generate an email. The email gets
  placed in the same directory as the xml. Only invoke from the root of the
  repository.

## Writing

Emails are composed in XML and validated against the schema located in
`resources/email.xsd`. xs3p-generated documentation can be found in
`docs/email.html` ([GitHub Pages](https://icdocsoc.github.io/mailgen/email.html)).

Most tags and attributes are optional. The generator defaults to sensible
defaults, or excludes elements.

The &lt;event&gt; event component is one of the more complex components. This
component will populate most of its field if a &lt;facebook&gt; tag is contained
within. Manually defining &lt;text&gt;, etc... will override the information
fetched from Facebook.

Also have a look at the examples in the `examples` directory.

## Generating Emails

`./mailgen.js <xml file>`

Once done, the script will output the path of the generated HTML file.

## Sending Emails

- Open the generated HTML file in Chrome.
- Select, copy and paste the contents into an email client of your choice. Apple
  Mail works well.
- Send.

## Updating Sponsors

- Modify the `SponsorType` in `resources/email.xsd`. This should be the same as
  the short form in the logo URL.
- Add a mapping from this short form in `src/sponsors.js` for the alt text.
- Update the logo collage on the website.

