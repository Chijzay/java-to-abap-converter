package de.example.j2abap.api;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;

@RestController
@RequestMapping("/api")
public class HealthController {
  @GetMapping({"/health", "/health/"})
  public ResponseEntity<String> health() { 
    return ResponseEntity.ok("ok"); 
  }
}

