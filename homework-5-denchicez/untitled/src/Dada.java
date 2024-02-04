import java.util.NoSuchElementException;
import java.util.Scanner;

public class Dada {
    private Scanner in;

    public void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        while (true) {
            int a = sc.nextInt();
            try {
                this.in = new Scanner(System.in);
                this.in = new Scanner(System.in);
            } catch (NoSuchElementException e) {
                System.err.println(e);
                this.in = new Scanner(System.in);
            }
        }
    }
}
