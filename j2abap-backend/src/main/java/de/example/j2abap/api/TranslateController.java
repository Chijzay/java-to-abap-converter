package de.example.j2abap.api;

import de.example.j2abap.JavaToAbapTranslator;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
public class TranslateController {

    private final JavaToAbapTranslator translator = new JavaToAbapTranslator();

    @PostMapping(value = {"/translate", "/translate/"}, consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.TEXT_PLAIN_VALUE)
    public String translate(
            @RequestParam(defaultValue = "auto") String mode,
            @RequestBody String javaCode
    ) {
        return translator.translate(javaCode, mode);
    }
}
