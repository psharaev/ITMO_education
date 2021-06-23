import java.util.Scanner;

public class B {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        int n = sc.nextInt();
        int k = -710 * 25000;

        while (n > 0) {
            n--;
            k += 710;
            System.out.println(k);
        }
    }
    
}
