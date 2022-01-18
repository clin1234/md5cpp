from urllib.request import urlopen
from urllib.response import addinfourl
from json import loads
from typing import Dict

with urlopen('https://html.spec.whatwg.org/entities.json') as entities_json:
    entities_json: addinfourl
    entities: Dict = loads(entities_json.read())

print(f'static constexpr std::array<entity,{len(entities)}> entity_table{{{{')
for k, v in entities.items():
    string = "\t{ "
    string += '"' + k + '", { '
    string += ','.join(str(x) for x in v['codepoints']) + ' }},'
    print(string)
print('}};')
