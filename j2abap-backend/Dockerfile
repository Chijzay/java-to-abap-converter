# -------- build stage --------
FROM maven:3.9.9-eclipse-temurin-21 AS build
WORKDIR /workspace

# Nur POM zuerst (Cache)
COPY j2abap-backend/pom.xml ./pom.xml
RUN mvn -q -DskipTests dependency:go-offline

# Dann Source
COPY j2abap-backend/src ./src

RUN echo "---- HealthController in build image ----" \
 && ls -la src/main/java/de/example/j2abap/api \
 && wc -l src/main/java/de/example/j2abap/api/HealthController.java \
 && nl -ba src/main/java/de/example/j2abap/api/HealthController.java | sed -n '1,120p'

RUN mvn -q -DskipTests package

# -------- runtime stage --------
FROM eclipse-temurin:21-jre
WORKDIR /app

# Das gebaute Jar aus dem build stage holen
COPY --from=build /workspace/target/*.jar /app/app.jar

EXPOSE 8080
ENTRYPOINT ["sh","-c","exec java -Dserver.port=${PORT:-8080} -jar /app/app.jar"]
