public class SumFloat {

    public static void main(String[] args) {
        float sum = 0;
        for (final String arg : args) {
            sum += calcSum(arg);
        }

        System.out.println(sum);
    }

    private static float calcSum(String s) {
        float res = 0;
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

            res += Float.parseFloat(s.substring(leftPos, rightPos));
            leftPos = rightPos;
        }

        return res;
    }
}
