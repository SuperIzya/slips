const core = require('@actions/core');
const fs = require('fs/promises');


core.debug('Starting updating README.md');
const github = JSON.parse(core.getInput('context', {required: true}));
core.debug(`github object is ${github}`);
const badgesJSON = core.getInput('badges', {required: true});
core.debug(`Passed badges configuration is ${badgesJSON} of type ${typeof badgesJSON}`);
const template = (() => {
    const res = core.getInput('template');
    if(!res) return 'README.tpl.md';
    else return res
})();


const buildBadge = ({ref_name, server_url, repository}) => ({name, yaml}) =>
    `[![${name}](${server_url}/${repository}/actions/workflows/${yaml}.yml/badge.svg?branch=${ref_name})](${server_url}/${repository}/actions/workflows/${yaml}.yml)`

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
