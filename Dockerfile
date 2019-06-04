FROM hseeberger/scala-sbt

COPY . ./

ARG ANONYMIZER_BOT_TOKEN
ARG BOT_ENVIRONMENT

CMD ["sbt", "run"]

