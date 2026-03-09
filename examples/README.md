# Examples

## Scripts

The [`scripts/`](scripts/) folder contains runnable [elm-pages scripts](https://elm-pages.com/docs/elm-pages-scripts/) that demonstrate `elm-ical` parsing against real-world iCal feeds.

## Running Scripts

- [Node.js](https://nodejs.org/) (v18+)
- [elm-pages](https://elm-pages.com/) installed via npm

Install dependencies from the `scripts/` directory:

```sh
cd examples/scripts
npm install
npx elm-pages run src/UsHolidays.elm
```

This fetches the Google Calendar US holidays feed and prints upcoming holidays:

```
US Holidays in 2026 (38 events):

  2026-01-01  Thu  New Year's Day
  2026-01-19  Mon  Martin Luther King Jr. Day
  ...
```
