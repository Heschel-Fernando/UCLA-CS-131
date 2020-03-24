import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
  private AtomicLongArray value;

  AcmeSafeState(int length) {
    value = new AtomicLongArray(length);
  }

  public int size() {
    return value.length();
  }

  public long[] current() {
    long[] res = new long[value.length()];
    for (var i = 0; i < res.length; i++) 
      res[i] = value.get(i);
    return res;
  }

  public void swap(int i, int j) {
    value.getAndDecrement(i);
    value.getAndIncrement(j);
  }
}