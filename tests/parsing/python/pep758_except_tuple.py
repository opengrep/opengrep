# PEP 758: unparenthesized except-tuples (Python 3.14+)

# Three bare-Name exceptions (always PEP 758: list has 2+ elements after comma)
try:
    pass
except ValueError, TypeError, RuntimeError:
    pass

# Four exceptions
try:
    pass
except ValueError, TypeError, RuntimeError, OSError:
    pass

# Two exceptions where second is dotted (PEP 758: Attribute, not Name)
try:
    pass
except ValueError, socket.error:
    pass

# Python 2 bind backward compat: single bare Name after comma
try:
    pass
except ValueError, e:
    pass

# Dotted type with Python 2 bind
try:
    pass
except socket.error, e:
    pass

# Two dotted exception names (both Attribute -> PEP 758)
try:
    pass
except http.client.HTTPException, urllib.error.URLError:
    pass

# Mixed: dotted + simple Names, 3+ items -> PEP 758
try:
    pass
except socket.error, ValueError, TypeError:
    pass

# Body with real statements
try:
    x = 1
except ValueError, TypeError, RuntimeError:
    y = 2
    print(y)

# Multiple handlers: parenthesized + unparenthesized + single + bare
try:
    pass
except (ValueError, TypeError):
    pass
except RuntimeError, OSError, KeyError:
    pass
except IOError:
    pass
except:
    pass

# Nested try/except both using PEP 758
try:
    try:
        pass
    except ValueError, TypeError, RuntimeError:
        pass
except OSError, IOError, KeyError:
    pass

# PEP 758 with else
try:
    pass
except ValueError, TypeError, RuntimeError:
    pass
else:
    pass

# PEP 758 with finally
try:
    pass
except ValueError, TypeError, RuntimeError:
    pass
finally:
    pass

# PEP 758 with else and finally
try:
    pass
except ValueError, TypeError, RuntimeError:
    pass
else:
    pass
finally:
    pass

# Subscript expressions as exception types (not Name -> PEP 758)
try:
    pass
except errors[0], errors[1]:
    pass

# Function call as exception type (not Name -> PEP 758 even with 1 after comma)
try:
    pass
except ValueError, get_error():
    pass
