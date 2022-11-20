const core = require('@actions/core');
const fs = require('fs/promises');


core.debug('Starting updating README.md');
const github = JSON.parse(core.getInput('context', {required: true}));
core.debug(`github object is ${github}`);
const badgesJSON = core.getInput('badges', {required: true});
core.debug(`Passed badges configuration is ${badgesJSON} of type ${typeof badgesJSON}`);
const template = (() => {
    const res = core.getInput('template');
    if(!res) return 'README.md.tpl';
    else return res
})();

const domain = (() => {
    const d = core.getInput('domain');
    if(!d) return 'https://github.com';
    else d
})();

const buildBadge = ({ref_name, repository}) => ({name, yaml}) =>
    `[![${name}](${domain}/${repository}/actions/workflows/${yaml}.yml/badge.svg?branch=${ref_name})](${domain}/${repository}/actions/workflows/${yaml}.yml?branch=${ref_name})`

async function run() {
    try {
        const badges = JSON.parse(`${badgesJSON}`).map(buildBadge(github)).reduce((a, b) => `${a} ${b}`, '');
        core.info(`The following badges will be added:
${badges}
`);
        const data = await fs.readFile(`${github.workspace}/${template}`, {encoding: 'utf8'});
        core.debug('Writing README.md');
        await fs.writeFile(`${github.workspace}/README.md`, `
${badges}
${data}
`);
    }
    catch (error) {
        core.setFailed(error.message);
    }
}
run();
