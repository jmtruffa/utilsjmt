library(telegram.bot)
library(logr)

tmp = file.path('/tmp/',"test.log")

lf = log_open(tmp)
#bot = Bot(token = bot_token("jmtTestBot"))
#print(bot$getMe())

updates = bot$getUpdates()

# chat_id = "CHAT_ID" # you can retrieve it from bot$getUpdates() after sending a message to the bot
# bot$sendMessage(chat_id = chat_id, text = "TestReply")


updater = Updater(token = Sys.getenv('R_TELEGRAM_BOT_jmtTestBot'))

ARGprices = function(bot, update){
  bot$send_message(chat_id = update$message$chat_id, text = "Me pongo a trabajar en ello. Dame unos segundos...")
  error = FALSE
  tryCatch(
    {
      fails =  botPricesArg()
    },
    error = function(e) {
      error <<- TRUE;
      print("Falló la llamada a la API")
      bot$send_message(chat_id = update$message$chat_id, text = "Falló la llamada a la API")
    }
  )
  if (!error) {
    bot$send_document(chat_id =update$message$chat_id,
                      document = '/tmp/pricesArg.xlsx')
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Archivo PricesArg.")
    # una vez enviado borramos el archivo
    file.remove('/tmp/pricesArg.xlsx')
    if (length(fails$ticker) != 0) {
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = paste0("Los tickers que fallaron son: ", toString(paste((fails$ticker)))))
    }
  }
  out = update$effective_user()
  out$call = "Prices Arg"
  log_print(out)
}

updater = updater + CommandHandler("ARGprices", ARGprices)
