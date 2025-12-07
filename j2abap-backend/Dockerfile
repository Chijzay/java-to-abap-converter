# -------- build stage --------
FROM maven:3.9.9-eclipse-temurin-21 AS build
WORKDIR /workspace

# Nur POM zuerst (Cache)
COPY j2abap-backend/pom.xml ./pom.xml
RUN mvn -q -DskipTests dependency:go-offline

# Dann Source
COPY j2abap-backend/src ./src

# Debug: zeig ob die Klasse da ist
RUN ls -la src/main/java/de/example/j2abap \
 && test -f src/main/java/de/example/j2abap/JavaToAbapTranslator.java \
 && echo "Translator file OK"

# -------- runtime stage --------
FROM eclipse-temurin:21-jre
WORKDIR /app

# Das gebaute Jar aus dem build stage holen
COPY --from=build /workspace/target/*.jar /app/app.jar

ENV PORT=8080
EXPOSE 8080

ENTRYPOINT ["sh","-c","exec java -Dserver.port=${PORT} -jar /app/app.jar"]
