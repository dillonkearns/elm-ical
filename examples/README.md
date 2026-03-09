# Examples

## Scripts

The [`scripts/`](scripts/) folder contains runnable [elm-pages scripts](https://elm-pages.com/docs/elm-pages-scripts/) that demonstrate `elm-ical` parsing against real-world iCal feeds.

### Prerequisites

- [Node.js](https://nodejs.org/) (v18+)
- [elm-pages](https://elm-pages.com/) installed via npm

Install dependencies from the `scripts/` directory:

```sh
cd examples/scripts
npm install
```

### Running

You can run a script directly from GitHub without cloning:

```sh
npx elm-pages run https://raw.githubusercontent.com/dillonkearns/elm-ical/main/examples/scripts/src/UsHolidays.elm
```

Or locally from the `examples/scripts/` directory:

```sh
npx elm-pages run src/UsHolidays.elm
```

This fetches the Google Calendar US holidays feed and prints upcoming holidays:

```
US Holidays in 2026 (38 events):

  2026-01-01  Thu  New Year's Day
  2026-01-19  Mon  Martin Luther King Jr. Day
  ...
```

### Available scripts

| Script | Description |
|--------|-------------|
| [`UsHolidays.elm`](scripts/src/UsHolidays.elm) | Parse Google Calendar US holidays and list 2026 events |
| [`ParseAirtable.elm`](scripts/src/ParseAirtable.elm) | Parse an Airtable calendar feed |
