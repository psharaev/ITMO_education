public class Sum {

    public static void main(String[] args) {
        int sum = 0;
        for (final String arg : args) {
            sum += calcSum(arg);
        }

        System.out.println(sum);
    }

    private static int calcSum(String s) {
        int res = 0;
        for (int leftPos = 0; leftPos < s.length(); ) {
            while (leftPos < s.length() && Character.isWhitespace(s.charAt(leftPos))) {
                leftPos++;
            }

            if (leftPos == s.length()) {
                break;
            }

            int rightPos = leftPos;
            while (rightPos < s.length() && !Character.isWhitespace(s.charAt(rightPos))) {
                rightPos++;
            }

            res += Integer.parseInt(s.substring(leftPos, rightPos));
            leftPos = rightPos;
        }

        return res;
    }
}
