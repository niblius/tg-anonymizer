IMG="anonymizer-img"

docker rm -f $(docker stop $(docker ps -a -q --filter ancestor="$IMG" --format="{{.ID}}")) && \
  docker build -t "$IMG" . && \
  docker run -d --env ANONYMIZER_BOT_TOKEN=$ANONYMIZER_BOT_TOKEN --env BOT_ENVIRONMENT="PRODUCTION" --name="anonymizer-$INSTANCE" anonymizer-img

