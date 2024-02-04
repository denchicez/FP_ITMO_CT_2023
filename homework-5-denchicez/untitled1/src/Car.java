public abstract class Car implements CarInterface {
    private final String carName, mark;

    public Car(String name, String mark) {
        this.carName = name;
        this.mark = mark;
    }
    public void run() {
        System.out.println("run");
        System.out.println(this.mark);
    }
}
