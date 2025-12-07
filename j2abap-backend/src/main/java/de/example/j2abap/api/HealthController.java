package de.example.j2abap.api;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class HealthController {

  @GetMapping(path = {"/api/health", "/api/health/"}, produces = MediaType.TEXT_PLAIN_VALUE)
  public ResponseEntity<String> health() {
    return ResponseEntity.ok("ok");
  }
}
