import java.util.Scanner;

public class GrundlagenDemo {

    // Einstiegspunkt
    public static void main(String[] args) {
        System.out.println("Willkommen zur Java-Grundlagendemo!");

        // Variablen und einfache Typen
        int zahl = 5;
        double preis = 19.99;
        boolean istAktiv = true;
        char buchstabe = 'A';
        String name = "Max";

        System.out.println("Name: " + name + ", Preis: " + preis);

        // Eingabe vom Benutzer
        Scanner scanner = new Scanner(System.in);
        System.out.print("Gib eine Zahl ein: ");
        int eingabe = scanner.nextInt();

        // if-Anweisung
        if (eingabe > 10) {
            System.out.println("Die Zahl ist größer als 10.");
        } else {
            System.out.println("Die Zahl ist kleiner oder gleich 10.");
        }

        // Schleife und Array
        int[] zahlen = {1, 2, 3, 4, 5};
        System.out.print("Array: ");
        for (int z : zahlen) {
            System.out.print(z + " ");
        }
        System.out.println();

        // switch-Anweisung
        switch (eingabe) {
            case 1 -> System.out.println("Du hast Eins eingegeben.");
            case 2 -> System.out.println("Du hast Zwei eingegeben.");
            default -> System.out.println("Eine andere Zahl wurde eingegeben.");
        }

        // Methodenaufruf
        int summe = berechneSumme(zahl, eingabe);
        System.out.println("Summe von " + zahl + " + " + eingabe + " = " + summe);

        // Objektorientierung
        Person person = new Person(name, 25);
        person.begruesse();

        scanner.close();
    }

    // Methode mit Parametern und Rückgabewert
    public static int berechneSumme(int a, int b) {
        return a + b;
    }
}

// Eigene Klasse mit Konstruktor und Methode
class Person {
    private String name;
    private int alter;

    public Person(String name, int alter) {
        this.name = name;
        this.alter = alter;
    }

    public void begruesse() {
        System.out.println("Hallo, ich bin " + name + " und " + alter + " Jahre alt.");
    }
}
