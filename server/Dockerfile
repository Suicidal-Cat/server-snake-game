FROM clojure:temurin-21-tools-deps

WORKDIR /app

COPY . .

RUN clj -P

EXPOSE 3000

CMD ["clj", "-M:-main"]
