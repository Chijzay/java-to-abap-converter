package de.example.j2abap.api;

import java.time.Instant;
import java.util.Map;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
// Wichtig: unterstützt /api/... UND ohne /api (falls dein Frontend /translate statt /api/translate nutzt)
@RequestMapping({"/api", ""})
public class HealthController {

  // Wichtig: Trailing Slash explizit erlauben
  @GetMapping(value = {"/health", "/health/"}, produces = MediaType.APPLICATION_JSON_VALUE)
  public Map<String, Object> health() {
    return Map.of(
        "status", "ok",
        "service", "j2abap-backend",
        "time", Instant.now().toString()
    );
  }
}
