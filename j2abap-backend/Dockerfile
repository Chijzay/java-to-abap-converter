# ---- build stage ----
FROM maven:3.9.9-eclipse-temurin-21 AS build
WORKDIR /app

# Nur pom zuerst (besseres Caching)
COPY pom.xml .
RUN mvn -q -DskipTests dependency:go-offline

# Dann Source
COPY src ./src
RUN mvn -q -DskipTests package

# ---- runtime stage ----
FROM eclipse-temurin:21-jre
WORKDIR /app

COPY --from=build /app/target/*.jar app.jar

# Koyeb setzt PORT automatisch – wir nutzen es.
ENV PORT=8080
EXPOSE 8080

ENTRYPOINT ["sh","-c","exec java -Dserver.port=${PORT:-8080} -jar /app/app.jar"]
