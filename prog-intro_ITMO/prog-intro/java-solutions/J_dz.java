import java.util.Scanner;

public class J_dz {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int n = scan.nextInt();
        scan.nextLine();

        int[][] a = new int[n][n];
        for (int i = 0; i < n; i++) {
            char[] chars = scan.nextLine().toCharArray();
            for (int j = 0; j < n; j++) {
                a[i][j] = chars[j] - '0';
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (a[i][j] == 1) {
                    System.out.print(1);
                    for (int k = j + 1; k < n; k++) {
                        a[i][k] -= a[j][k];
                        if (a[i][k] < 0) {
                            a[i][k] += 10;
                        }
                    }
                } else {
                    System.out.print(0);
                }
            }
            System.out.println();
        }
    }
}
