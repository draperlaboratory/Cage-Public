import java.util.logging.LogRecord;
import java.util.logging.Formatter;

public class SimplestFormatter extends Formatter {

        @Override
        public String format(LogRecord record){
            return record.getLevel() + ": " + record.getMessage() + "\n";
        }
}
