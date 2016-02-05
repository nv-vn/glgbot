# glgbot

## How to use:
```
$ echo -n "$TELEGRAM_API_TOKEN" > bot.token
$ oasis setup -setup-update dynamic
$ make
$ ./bot.native
```

## Customizing glgbot:
In order to add new commands, you can use the interface provided by `Api.Command`.
Your bot module should contain a `commands : Api.Command.command list`. For a basic example
of two of these types of commands, see src/bot.ml.
