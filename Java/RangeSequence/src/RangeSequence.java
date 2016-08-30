import java.io.IOException;
// Demonstrate the Java version of an F# "range sequence"
public class RangeSequence {
   private int mStart, mEnd;

   public RangeSequence(int start, int end) {
      mStart = start;
      mEnd = end;
   }

   // This iterator can "walk" through the sequence from start to end.
   // But how many ints are ever actually in memory?
   private class RangeSequenceIterator {
      private int mCurrent = mStart;
      public boolean hasNext() {
         return mCurrent <= mEnd;
      }
      public int next() {
         return mCurrent++;
      }
   }

   public RangeSequenceIterator iterator() {
      return new RangeSequenceIterator();
   }
   
   public static void main(String[] args) throws IOException {
      RangeSequence seq = new RangeSequence(1, 100000000);
      RangeSequenceIterator itr = seq.iterator();
      int largest = Integer.MIN_VALUE;
      while (itr.hasNext()) {
         int i = itr.next();
         largest = i;
      }
      System.out.println(largest);
      System.in.read();
   }
}
