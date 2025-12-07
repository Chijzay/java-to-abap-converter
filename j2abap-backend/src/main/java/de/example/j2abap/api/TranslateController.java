package de.example.j2abap.api;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
public class TranslateController {

  private final JavaToAbapTranslator translator = new JavaToAbapTranslator();

  @PostMapping(
      path = {"/translate", "/translate/"},
      consumes = MediaType.TEXT_PLAIN_VALUE,
      produces = MediaType.TEXT_PLAIN_VALUE
  )
  public ResponseEntity<String> translate(
      @RequestParam(name = "mode", defaultValue = "auto") String mode,
      @RequestBody(required = false) String javaCode
  ) {
    if (javaCode == null) javaCode = "";
    return ResponseEntity.ok(translator.translate(javaCode, mode));
  }
}
