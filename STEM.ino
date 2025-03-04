#include <LiquidCrystal.h>


  const int redLED = 2;
const int yellowLED = 3;
const int greenLED = 4;

const int trigPin = 5;
const int echoPin = 6;
const int buzzer = 7;

LiquidCrystal lcd(8, 9, 10, 11, 12, 13);


unsigned long previousMillis = 0;
const long greenLightTime = 10000;  
const long yellowLightTime = 3000; 
const long redLightTime = 10000;    


enum TrafficLightState { GREEN, YELLOW, RED };
TrafficLightState currentState = GREEN;

void setup() {
  
  pinMode(redLED, OUTPUT);
  pinMode(yellowLED, OUTPUT);
  pinMode(greenLED, OUTPUT);

 
  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
  pinMode(buzzer, OUTPUT);

  
  lcd.begin(16, 2); 
  lcd.setCursor(0, 0);
  lcd.print("DESIGNED BY XISC");
  lcd.setCursor(0, 1);
  lcd.print("SHIVANG AYUSH");
  delay(5000);
  lcd.clear();
}

void loop() {
  unsigned long currentMillis = millis();

  switch (currentState) {
    case GREEN:
      digitalWrite(greenLED, HIGH);
      digitalWrite(yellowLED, LOW);
      digitalWrite(redLED, LOW);

      lcd.setCursor(0, 0);
      lcd.print("    GREEN       ");
      lcd.setCursor(0, 1);
      lcd.print("SAFETY PRIORITY     ");

      if (currentMillis - previousMillis >= greenLightTime) {
        previousMillis = currentMillis;
        currentState = YELLOW;
      }
      break;

    case YELLOW:
      digitalWrite(greenLED, LOW);
      digitalWrite(yellowLED, HIGH);
      digitalWrite(redLED, LOW);

      lcd.setCursor(0, 0);
      lcd.print("    YELLOW       ");
      lcd.setCursor(0, 1);
      lcd.print("  GET READY!       ");

      if (currentMillis - previousMillis >= yellowLightTime) {
        previousMillis = currentMillis;
        currentState = RED;
      }
      break;

    case RED:
      digitalWrite(greenLED, LOW);
      digitalWrite(yellowLED, LOW);
      digitalWrite(redLED, HIGH);

      lcd.setCursor(0, 0);
      lcd.print("     RED       ");
      lcd.setCursor(0, 1);
      lcd.print("  STOP HERE!       ");

      
      if (detectVehicle()) {
        digitalWrite(buzzer, HIGH);
        delay(500);
        digitalWrite(buzzer, LOW);
      }

      if (currentMillis - previousMillis >= redLightTime) {
        previousMillis = currentMillis;
        currentState = GREEN;
      }
      break;
  }
}


bool detectVehicle() {
  digitalWrite(trigPin, LOW);
  delayMicroseconds(2);
  digitalWrite(trigPin, HIGH);
  delayMicroseconds(10);
  digitalWrite(trigPin, LOW);

  long duration = pulseIn(echoPin, HIGH);
  int distance = duration * 0.034 / 2;

  if (distance > 0 && distance < 9) {
        return true;
  }
  return false;
}
