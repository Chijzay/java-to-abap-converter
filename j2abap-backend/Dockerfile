# ---- build stage ----
FROM eclipse-temurin:21-jdk AS build
WORKDIR /app

# Maven Wrapper + POM aus dem Backend-Unterordner holen
COPY j2abap-backend/.mvn/ .mvn/
COPY j2abap-backend/mvnw j2abap-backend/pom.xml ./
RUN chmod +x mvnw && ./mvnw -q -DskipTests dependency:go-offline

# Source
COPY j2abap-backend/src/ src/
RUN ./mvnw -q -DskipTests package

# ---- runtime stage ----
FROM eclipse-temurin:21-jre
WORKDIR /app
COPY --from=build /app/target/*.jar app.jar

ENV PORT=8080
EXPOSE 8080
ENTRYPOINT ["sh","-c","exec java -Dserver.port=${PORT} -jar /app/app.jar"]
