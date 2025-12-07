# -------- build stage --------
FROM maven:3.9.9-eclipse-temurin-21 AS build
WORKDIR /workspace

# Nur POM zuerst (Cache)
COPY j2abap-backend/pom.xml ./pom.xml
RUN mvn -DskipTests dependency:go-offline

# Dann Source
COPY j2abap-backend/src ./src

# --- DEBUG: zeig was wirklich im Container liegt ---
RUN set -eux; \
  echo "---- LIST api dir ----"; \
  ls -la src/main/java/de/example/j2abap/api; \
  echo "---- FIND HealthController.java ----"; \
  find src -name "HealthController.java" -print; \
  echo "---- wc -l ----"; \
  wc -l src/main/java/de/example/j2abap/api/HealthController.java; \
  echo "---- show with line numbers ----"; \
  nl -ba src/main/java/de/example/j2abap/api/HealthController.java | sed -n '1,120p'; \
  echo "---- show weird chars (tail) ----"; \
  tail -n 40 src/main/java/de/example/j2abap/api/HealthController.java | cat -A; \
  echo "---- grep conflict markers / duplicates ----"; \
  grep -nE '<<<<<<|>>>>>>|======|package |import ' src/main/java/de/example/j2abap/api/HealthController.java || true

RUN mvn -DskipTests package

# -------- runtime stage --------
FROM eclipse-temurin:21-jre
WORKDIR /app

COPY --from=build /workspace/target/*.jar /app/app.jar

ENV PORT=8080
EXPOSE 8080
ENTRYPOINT ["sh","-c","exec java -Dserver.port=${PORT} -jar /app/app.jar"]
