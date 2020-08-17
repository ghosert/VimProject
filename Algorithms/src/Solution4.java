import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

public class Solution4 {

    public static void main(String[] args) {
        System.out.println("hello world!");

        try {
            String filterValue = "[NOW-1 TO NOW]";
            // SALEDATE%3A%5BNOW-1%20TO%20NOW%5D
            int rangeIndex = filterValue.indexOf("TO");
            if (rangeIndex > 0) {
                String from = filterValue.substring(0, rangeIndex);
                from = from.replaceAll("\\[", "").trim();
                String to = filterValue.substring(rangeIndex + 2, filterValue.length());
                to = to.replaceAll("\\]", "").trim();
                GregorianCalendar xmlDate = convertToXMLCalendar(from);
                if (xmlDate != null) {
                    System.out.println("set " + xmlDate);
                }
                xmlDate = convertToXMLCalendar(to);
                if (xmlDate != null) {
                    System.out.println("set " + xmlDate);
                }
            } else {
                filterValue = filterValue.replaceAll("[\\[\\]]", "").trim();
                GregorianCalendar xmlDate = convertToXMLCalendar(filterValue);
                if (xmlDate != null) {
                    Calendar saleEndDate = xmlDate;
                    System.out.println("set " + saleEndDate);
                }
            }
        } catch (ParseException pe) {
            System.out.println("Say something.");
        }
    }

    private static GregorianCalendar convertToXMLCalendar(String dateStr) throws ParseException {
        GregorianCalendar cal = new GregorianCalendar();
        if(dateStr.trim().equalsIgnoreCase("NOW")){
            Calendar utcNowCal = Calendar.getInstance();
            cal.setTimeInMillis(utcNowCal.getTimeInMillis());
        }else if (dateStr.trim().equalsIgnoreCase("NOW-1")){
            Calendar utcNowCal = Calendar.getInstance();
            utcNowCal.add(Calendar.HOUR_OF_DAY, -24);
            cal.setTimeInMillis(utcNowCal.getTimeInMillis());
        } else if(dateStr.trim().equalsIgnoreCase("TODAY")){
            Calendar currentDate = Calendar.getInstance();;
            currentDate.set(Calendar.HOUR_OF_DAY, 0); //anything 0 - 23
            currentDate.set(Calendar.MINUTE, 0);
            currentDate.set(Calendar.SECOND, 0);
            cal.setTimeInMillis(currentDate.getTimeInMillis());
        } else{
            DateFormat format = new SimpleDateFormat("yyyy-MM-dd");
            Date date = format.parse(dateStr);
            cal.setTime(date);
        }
        return cal;
    }
}
