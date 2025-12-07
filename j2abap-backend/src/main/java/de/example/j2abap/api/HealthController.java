package de.example.j2abap.api;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
public class HealthController {

  @GetMapping(
      path = {"/health", "/health/"},
      produces = MediaType.TEXT_PLAIN_VALUE
  )
  public ResponseEntity<String> health() {
    return ResponseEntity.ok("OK");
  }
}
