#include <Servo.h>
#include <LiquidCrystal.h>
LiquidCrystal lcd(7,8,9,10,11,12);
int buttonpin = 2;
int mpin = 4;
Servo myServo;
int led2pin = 6;
int bpin = 13;

void setup(){
  pinMode(bpin, OUTPUT);
  pinMode(buttonpin, INPUT);
  pinMode(mpin,OUTPUT);
  pinMode(led2pin, OUTPUT);
  myServo.attach(5);

  lcd.begin(16, 2);
  lcd.setCursor(0, 0);
  lcd.print("Insert Sanitary Pad");
  lcd.setCursor(0,1);
  lcd.print("Press the button");
  digitalWrite(bpin, LOW);
  digitalWrite(mpin, LOW);
  digitalWrite(led2pin, LOW);
  myServo.write(0);





}

void loop() {
  if (digitalRead(buttonpin) == HIGH)
  {
    digitalWrite(bpin, HIGH);
    delay(100);
    digitalWrite(bpin, LOW);
    countdown();
    lcd.clear();
    lcd.setCursor(0, 0);
    lcd.print("In Progress");
    myServo.write(180);
    digitalWrite(mpin, LOW);
    digitalWrite(led2pin, HIGH);
    delay(25000);
    digitalWrite(mpin, LOW);
    myServo.write(0);
    digitalWrite(led2pin, LOW);
    lcd.clear();
    lcd.setCursor(0, 0);
    count2down();
    lcd.clear();
    lcd.print("Dispose Ashes");
    digitalWrite(bpin, HIGH);
    delay(2000);
    digitalWrite(bpin, LOW);

    delay(10000);
    lcd.clear();
    lcd.setCursor(0, 0);
    lcd.print("Insert Sanitary Pad");
    lcd.setCursor(0, 1);
    lcd.print("Press the button");

  }
}
void countdown(){
  for(int i = 10;i >= 0; i--){
    lcd.clear();
    lcd.setCursor(0, 0);
    lcd.print("Countdown:");
    lcd.print(i);
    digitalWrite(bpin, HIGH);
    delay(100);
    digitalWrite(bpin, LOW);
    delay(900);
    
  }

}
void count2down(){
  for(int j = 90;j >= 0; j--){
    lcd.clear();
    lcd.setCursor(0,0);
    lcd.print("Please Wait");
    lcd.setCursor(0,1);
    lcd.print(j);

    delay(1000);
  }
}
