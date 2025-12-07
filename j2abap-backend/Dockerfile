# ---- build stage ----
FROM eclipse-temurin:21-jdk AS build
WORKDIR /app

# Maven Wrapper + POM zuerst (bessere Cache-Nutzung)
COPY .mvn/ .mvn/
COPY mvnw pom.xml ./
RUN chmod +x mvnw && ./mvnw -q -DskipTests dependency:go-offline

# Source danach
COPY src/ src/
RUN ./mvnw -q -DskipTests package

# ---- runtime stage ----
FROM eclipse-temurin:21-jre
WORKDIR /app

COPY --from=build /app/target/*.jar app.jar

ENV PORT=8080
EXPOSE 8080

# Spring Boot muss auf $PORT hören (Fly routet darauf)
ENTRYPOINT ["sh","-c","exec java -Dserver.port=${PORT} -jar /app/app.jar"]
