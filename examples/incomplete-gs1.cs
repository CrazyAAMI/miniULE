struct Gs1Rule {
    char[4] Prefix;
    byte Min;
    byte Max;
}

const Gs1Rule[12] rules = [
    Gs1Rule { Prefix: "01", Min: 0, Max: 0 },
    Gs1Rule { Prefix: "01", Min: 0, Max: 0 },
    Gs1Rule { Prefix: "01", Min: 0, Max: 0 },
    Gs1Rule { Prefix: "01", Min: 0, Max: 0 },
    Gs1Rule { Prefix: "01", Min: 0, Max: 0 },
    Gs1Rule { Prefix: "01", Min: 0, Max: 0 },
];



//const char GROUP_SEPARATOR = '\xFF'; // GS - ASCII non printable character
const char GROUP_SEPARATOR = '['; // standard replacement GS (?) printable character

bool HandleNextRule(int index, &string buffer) {
    for rule in rules {
        if (buffer.StartsWith(rule.Prefix)) {
            *buffer = Right(buffer, StrLen(buffer) - StrLen(rule.Prefix))
            return true;
        }
    }
    return false;
}

entry(input) {
    int attempts = 0;
    string buffer = input;

    while ((StrLen(buffer) > 0) && (attempts++ < 50)) {
        HandleNextRule(buffer);
    }
}