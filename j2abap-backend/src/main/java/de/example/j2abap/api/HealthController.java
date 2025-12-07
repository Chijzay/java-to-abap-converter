package de.example.j2abap.api;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
public class HealthController {

  // Damit die Startseite NICHT "Whitelabel 404" zeigt:
  @GetMapping({"/"})
  public Map<String, String> index() {
    return Map.of(
        "status", "ok",
        "service", "java-to-abap-backend",
        "hint", "Try /api/health or POST /api/translate?mode=auto"
    );
  }

  // Wichtig: beide Varianten zulassen (mit und ohne Slash am Ende)
  @GetMapping({"/api/health", "/api/health/"})
  public Map<String, String> health() {
    return Map.of("status", "ok");
  }
}
