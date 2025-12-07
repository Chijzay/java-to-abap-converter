package de.example.j2abap.api;

import de.example.j2abap.JavaToAbapTranslator;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
public class TranslateController {

  private final JavaToAbapTranslator translator = new JavaToAbapTranslator();

  @PostMapping(value = {"/translate", "/translate/"}, consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.TEXT_PLAIN_VALUE)
  public ResponseEntity<String> translate(
      @RequestParam(name = "mode", defaultValue = "auto") String mode,
      @RequestBody String input
  ) {
    if (input == null || input.isBlank()) return ResponseEntity.ok("");
    if (input.length() > 200_000) return ResponseEntity.badRequest().body("Input too large.");

    String m = mode == null ? "auto" : mode.trim().toLowerCase();

    return switch (m) {
      case "snippet" -> ResponseEntity.ok(translator.translateSnippet(input));
      case "class" -> ResponseEntity.ok(translator.translateClass(input));
      case "auto" -> ResponseEntity.ok(translator.translateAuto(input));
      default -> ResponseEntity.badRequest().body("Unknown mode. Use: auto | snippet | class");
    };
  }
}
