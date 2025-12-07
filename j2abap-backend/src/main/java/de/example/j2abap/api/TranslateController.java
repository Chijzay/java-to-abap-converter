package de.example.j2abap.api;

import de.example.j2abap.JavaToAbapTranslator;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
public class TranslateController {

  private final JavaToAbapTranslator translator;

  public TranslateController(JavaToAbapTranslator translator) {
    this.translator = translator;
  }

  @GetMapping("/ping")
  public ResponseEntity<String> ping() {
    return ResponseEntity.ok("pong");
  }

  @PostMapping(path = {"/translate", "/translate/"})
  public ResponseEntity<String> translate(
      @RequestParam(name = "mode", defaultValue = "auto") String mode,
      @RequestBody(required = false) String javaCode
  ) {
    if (javaCode == null) javaCode = "";
    return ResponseEntity.ok(translator.translate(javaCode, mode));
  }
}
