#include <Arduino.h>

/*
 * Normalize MAC accepts a MAC address, and returns a lowercase string with all
 * colon delimeters removed.
 */
String normalize_mac(String str)
{
  str.toLowerCase();
  String out = "";
  for (char& c : str)
  {
    if (c != ':')
      out += c;
  }
  return out;
}
