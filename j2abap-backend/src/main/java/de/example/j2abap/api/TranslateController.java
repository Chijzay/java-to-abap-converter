package de.example.j2abap.api;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping({"/api", ""})
public class TranslateController {

  private final JavaToAbapTranslator translator = new JavaToAbapTranslator();

  @PostMapping({"/translate", "/translate/"})
  public ResponseEntity<String> translate(
      @RequestParam(defaultValue = "auto") String mode,
      @RequestBody String javaCode
  ) {
    String abap;
    switch (mode.toLowerCase()) {
      case "snippet" -> abap = translator.translateSnippet(javaCode);
      case "class" -> abap = translator.translateClass(javaCode);
      case "auto" -> abap = translator.translateAuto(javaCode);
      default -> abap = "ERROR: unknown mode. Use auto|snippet|class.";
    }
    return ResponseEntity.ok(abap);
  }
}
