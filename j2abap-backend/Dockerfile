# ---- build stage ----
FROM maven:3.9.9-eclipse-temurin-21 AS build
WORKDIR /app

# Erst nur pom.xml kopieren (Caching)
COPY j2abap-backend/pom.xml ./pom.xml
RUN mvn -q -DskipTests dependency:go-offline

# Dann Source kopieren und bauen
COPY j2abap-backend/src ./src
RUN mvn -q -DskipTests package

# ---- runtime stage ----
FROM eclipse-temurin:21-jre
WORKDIR /app
COPY --from=build /app/target/*.jar /app/app.jar

ENV PORT=8080
EXPOSE 8080
ENTRYPOINT ["sh","-c","exec java -Dserver.port=${PORT} -jar /app/app.jar"]
