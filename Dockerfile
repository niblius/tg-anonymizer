FROM hseeberger/scala-sbt

COPY . ./

ARG ANONYMIZER_BOT_TOKEN

CMD ["sbt", "run"]

