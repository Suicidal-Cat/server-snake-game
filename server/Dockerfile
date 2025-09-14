FROM clojure:lein-2.11.2

WORKDIR /app

COPY . .

RUN lein deps

EXPOSE 3000

CMD ["lein", "run"]
