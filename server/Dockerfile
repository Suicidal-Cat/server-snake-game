FROM clojure:openjdk-20-tools-deps

WORKDIR /app

COPY . .

RUN clj -A:deps install

EXPOSE 3000

CMD ["clj", "-M:run"]