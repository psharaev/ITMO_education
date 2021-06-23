public class Sum {

    public static void main(String[] args) {

        int sum = 0;

        for (final String arg : args) {
            int startIndex = -1;

            for (int i = 0; i < arg.length(); i++) {
                if (Character.isWhitespace(arg.charAt(i))) {
                    if (startIndex != -1) {
                        sum += Integer.parseInt(arg.substring(startIndex, i));
                        startIndex = -1;
                    }
                } else if (startIndex == -1) {
                    startIndex = i;
                }

            }

            if (startIndex != -1) {
                sum += Integer.parseInt(arg.substring(startIndex));
            }
        }

        System.out.println(sum);
    }
}
