package de.example.j2abap.api;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.Map;

@RestController
@CrossOrigin(
    origins = {
        "https://chijzay.github.io",
        "http://localhost:5173",
        "http://127.0.0.1:5173"
    }
)
public class HealthController {

  @GetMapping(
      path = {"/api/health", "/api/health/"},
      produces = MediaType.APPLICATION_JSON_VALUE
  )
  public Map<String, Object> health() {
    Map<String, Object> out = new LinkedHashMap<>();
    out.put("status", "ok");
    out.put("service", "java-to-abap-api");
    out.put("time", Instant.now().toString());
    return out;
  }
}
